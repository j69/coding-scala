/* Copyright (C) 2010-2019 Escalate Software, LLC. All rights reserved. */

package udemy.advanced3.module12.support

object BlankValues {
  class ReplaceWithCorrectException extends Exception

  val __ = "Should be filled in"

  class ___ extends ReplaceWithCorrectException {
    override def toString() = "___"
  }
}
