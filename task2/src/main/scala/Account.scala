import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

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

  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
