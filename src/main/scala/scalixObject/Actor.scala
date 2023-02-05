package fr.leroyer.athimon
package scalixObject

class Actor(val name:String, val surname : String, var id: Int = -1) {

  override def toString = name + " " + surname
}
