package fr.leroyer.athimon

import scala.annotation.unused

class FullName(val firstName: String, val lastName: String) {
  override def toString: String = s"$firstName+$lastName"

  def getFullName: String = s"$firstName+$lastName"
}