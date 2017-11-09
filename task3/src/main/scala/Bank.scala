import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    val accountId = accountCounter.get()
    if(accountCounter.compareAndSet(accountId, accountId+1))
      return BankManager.createAccount(""+(accountId+1), bankId, initialBalance)
    createAccount(initialBalance)
  }

  def findAccount(accountId: String): ActorRef = {
    // Use BankManager to look up an account with ID accountId
    BankManager.findAccount(bankId, accountId)
  }

  def findOtherBank(bankId: String): ActorRef = {
    // Use BankManager to look up a different bank with ID bankId
    BankManager.findBank(bankId)
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
    case GetAccountRequest(id) => sender ! findAccount(id) // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)

    case t: TransactionRequestReceipt => {
      val isInternal = t.toAccountNumber.length <= 4
      val toBankId = if (isInternal) bankId else t.toAccountNumber.substring(0, 4)
      val toAccountId = if (isInternal) t.toAccountNumber else t.toAccountNumber.substring(4)

      if (isInternal || toBankId == bankId) {
        findAccount(toAccountId) ! t
      }
      else {
        findOtherBank(toBankId) ! t
      }
    }

    case msg => ???
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout = new Timeout(5 seconds)
    val isInternal = t.to.length <= 4
    val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
    val toAccountId = if (isInternal) t.to else t.to.substring(4)
    val transactionStatus = t.status

    if (isInternal || toBankId == bankId) {
      val toAccountOption = findAccount(toAccountId)
      if(toAccountOption != null)
        toAccountOption ! t
      else{
        t.status = TransactionStatus.FAILED
        findAccount(t.from.substring(4)) ! new TransactionRequestReceipt(t.from, t.id, t)
      }
    }
    else {
      val toBankOption = findOtherBank(toBankId)
      if(toBankOption != null)
        toBankOption ! t
      else {
        t.status = TransactionStatus.FAILED
        findAccount(t.from.substring(4)) ! new TransactionRequestReceipt(t.from, t.id, t)
      }
    }
  }
}