package annot

import stainless.collection.*
import stainless.annotation.*
import stainless.lang.*
import utils.Utils.*

// https://github.com/ElCep/DSCATT/blob/c249e756e9b475c95d6eeeabb7f8813a79a6ac7f/model/scala/src/main/scala/dscatt/Croping.scala

sealed trait Crop

case object Mil extends Crop

case object Peanut extends Crop

case object Fallow extends Crop

case object NotAssigned extends Crop

// https://github.com/ElCep/DSCATT/blob/c249e756e9b475c95d6eeeabb7f8813a79a6ac7f/model/scala/src/main/scala/dscatt/World.scala#L96

object World {
  def farmedParcelsForKitchen(parcels: List[Parcel], kitchen: Kitchen): List[Parcel] = parcels.filter(_.farmerID == kitchen.id)

  def farmedParcelsForKitchen(world: World, kitchen: Kitchen): List[Parcel] = farmedParcelsForKitchen(world.parcels, kitchen)
}


case class World(parcels: List[Parcel])

// https://github.com/ElCep/DSCATT/blob/c249e756e9b475c95d6eeeabb7f8813a79a6ac7f/model/scala/src/main/scala/dscatt/Parcel.scala


case class Parcel(farmerID: Kitchen.KitchenID, crop: Crop, area: Double) {
  require(area.isFinite && area >= 0)
}

object Parcel {
  def isCultivated(parcel: Parcel) = parcel.crop match {
    case Peanut | Mil => true
    case _=> false
  }
}

// https://github.com/ElCep/DSCATT/blob/c9d2cb1181d13d07fdc4bd74d73d1e3c9d1857c4/model/scala/src/main/scala/dscatt/Kitchen.scala


case class Kitchen(id: Kitchen.KitchenID)

object Kitchen {

  type KitchenID = Int

  // TO SPECIFY: 209
  def cultivatedSurface(world: World, kitchen: Kitchen): Double = {
    val cultivatedParcels = World.farmedParcelsForKitchen(world, kitchen).filter(p => Parcel.isCultivated(p))
    map_forall(cultivatedParcels, (p: Parcel) => p.area, (a: Double) => !a.isNaN && a >= 0)
    cultivatedParcels.map(_.area).sum
  }.ensuring(res => res >= 0)
}