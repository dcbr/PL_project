object Bank {

  private var idCounter: Int = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = {
    if(from.uid<to.uid)// Prevent deadlock
      from.synchronized{to.synchronized{
        from.withdraw(amount)// Withdraw will throw exception if invalid amount/insufficient funds
        to.deposit(amount)// Deposit cannot fail anymore (since withdraw would have thrown invalid amount exception otherwise)
      }}
    else
      to.synchronized{from.synchronized{
        from.withdraw(amount)
        to.deposit(amount)
      }}
  }

  def getUniqueId: Int = this.synchronized{
    idCounter += 1 // Can this be improved? -> yes, use synchronization to prevent duplicate 'unique' ids in multithreaded environment
    idCounter
  }

}
