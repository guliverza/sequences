import org.scalatest._

class AppSpec
  extends FlatSpec
  with Matchers {

  "sequences" must "detect 4 from 1,2,3" in {
    App.calcNext(List(1, 2, 3)) should be (Some(4))
    App.calcNext(List(3, 2, 1)) should be (Some(0))
  }

  "sequences" must "detect 8 from 2,4,6" in {
    App.calcNext(List(2, 4, 6)) should be (Some(8))
    App.calcNext(List(4, 6, 8, 10)) should be (Some(12))
    App.calcNext(List(12, 10, 8)) should be (Some(6))
  }

  "sequences" must "detect 16 from 2,4,8" in {
    App.calcNext(List(1, 2, 4)) should be (Some(8))
    App.calcNext(List(2, 4, 8)) should be (Some(16))
    App.calcNext(List(2, 4, 8, 16)) should be (Some(32))
    App.calcNext(List(128, 64, 32)) should be (Some(16))
    App.calcNext(List(1, 11, 111, 1111)) should be (Some(11111))
  }

  "sequences" must "detect 8 from 1,1,2,3,5" in {
    App.calcNext(List(1, 1, 2, 3, 5)) should be (Some(8))
    App.calcNext(List(7, 7, 14, 21, 35)) should be (Some(56))
    App.calcNext(List(2, 3, 5, 8, 13)) should be (Some(21))
    App.calcNext(List(35, 21, 14, 7, 7)) should be (Some(0))
  }

}
