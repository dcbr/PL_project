import exceptions._

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
  private var balance: Double = initialBalance
  def withdraw(amount: Double): Unit = this.synchronized {
    if (amount < 0) throw new IllegalAmountException("You cannot withdraw a negative amount of money!")
    if (amount > balance) throw new NoSufficientFundsException("Your account has insufficient funds to withdraw this amount of money.")
    balance -= amount
  }
  def deposit(amount: Double): Unit = this.synchronized {
    if (amount < 0) throw new IllegalAmountException("You cannot deposit a negative amount of money!")
    balance += amount
  }
  def getBalanceAmount: Double = balance
}
