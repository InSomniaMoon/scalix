package fr.leroyer.athimon
package scalixObject

class Movie (val id : Int, val title : String){
  override def toString :String = {
    return s"Movie $id : $title";
  }
}
