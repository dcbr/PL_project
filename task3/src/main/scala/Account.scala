import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Should return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    // Should return whether all Transaction-objects in transactions are completed
    for(t <- transactions.values)
      if(!t.isCompleted) return false
    true
  }

  def withdraw(amount: Double): Unit = balance.synchronized {
    if (amount < 0) throw new IllegalAmountException("You cannot withdraw a negative amount of money!")
    if (amount > balance.amount) throw new NoSufficientFundsException("Your account has insufficient funds to withdraw this amount of money.")
    balance.amount -= amount
  }
  def deposit(amount: Double): Unit = balance.synchronized {
    if (amount < 0) throw new IllegalAmountException("You cannot deposit a negative amount of money!")
    balance.amount += amount
  }
  def getBalanceAmount: Double = balance.amount

  def sendTransactionToBank(t: Transaction): Unit = {
    // Should send a message containing t to the bank of this account
    BankManager.findBank(bankId) ! t
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }

    t

  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      transaction.receiptReceived = true
      if (!transaction.isSuccessful) {
        deposit(transaction.amount)
      }
      transactions += (transaction.id -> transaction)
    }

    case BalanceRequest => getBalanceAmount // Should return current balance

    case t: Transaction => {
      // Handle incoming transaction
      deposit(t.amount)
      t.status = TransactionStatus.SUCCESS
      BankManager.findBank(bankId) ! new TransactionRequestReceipt(t.from, t.id, t)
    }

    case msg => ???
  }


}
