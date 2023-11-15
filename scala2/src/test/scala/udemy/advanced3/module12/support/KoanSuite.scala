/* Copyright (C) 2010-2019 Escalate Software, LLC. All rights reserved. */

package udemy.advanced3.module12.support

import org.scalatest.funsuite.AnyFunSuite

abstract class KoanSuite extends AnyFunSuite {
  def koan(name: String)(fun: => Unit) = test(name)(fun)
}
