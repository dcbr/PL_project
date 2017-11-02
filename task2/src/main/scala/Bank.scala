import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

  private val uid = new AtomicInteger(0)
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = new ForkJoinPool

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }

  @tailrec final def generateAccountId: Int = {// This is the 'compare and swap' approach, since uid had to be a val instead of var
    val current=uid.get
    val updated=current+1
    if(uid.compareAndSet(current,updated))
      return updated
    generateAccountId
  }

  private def processTransactions: Unit = ???

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

}
