package fr.leroyer.athimon

import scala.annotation.unused

class FullName(val firstName: String, val lastName: String) {
  override def toString: String = "$firstName+$lastName"
  def getFullName: String = "$firstName+$lastName"
}