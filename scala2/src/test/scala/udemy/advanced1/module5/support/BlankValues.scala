/* Copyright (C) 2010-2017 Escalate Software, LLC. All rights reserved. */

package udemy.advanced1.module5.support

object BlankValues {
  class ReplaceWithCorrectException extends Exception

  val __ = "Should be filled in"

  class ___ extends ReplaceWithCorrectException {
    override def toString() = "___"
  }
}
