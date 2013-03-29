package chapter8

trait Prop { self =>
  def check: Boolean
  def &&(other: Prop): Prop = new Prop {
    def check = self.check && other.check 
  }
}