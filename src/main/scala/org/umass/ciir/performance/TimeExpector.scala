package org.umass.ciir.performance


class TimeExpector(totalCount : Int) {

  var firstTick : Long = -1
  var tickCount = 0
  var terminate :Boolean = false
  def tick(): Unit ={
    if( firstTick < 0){
      firstTick = System.currentTimeMillis()
      tickCount
    }
    else if(!terminate){
      tickCount = tickCount + 1
      val currentTime = System.currentTimeMillis()
      if (currentTime - firstTick > 10000){
        val esitmateTime = totalCount * (currentTime - firstTick) / tickCount
        println("estimate time : %d sec".format(esitmateTime / 1000))
        terminate = true
      }
    }
  }
}
