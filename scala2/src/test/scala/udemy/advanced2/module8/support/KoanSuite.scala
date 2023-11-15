/* Copyright (C) 2010-2018 Escalate Software, LLC. All rights reserved. */

package udemy.advanced2.module8.support

import org.scalatest.funsuite.AnyFunSuite

abstract class KoanSuite extends AnyFunSuite {
  def koan(name: String)(fun: => Unit) = test(name)(fun)
}
