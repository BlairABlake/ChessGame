package com.blairablake

object Color {
  sealed trait Color
  case object White extends Color
  case object Black extends Color
}
