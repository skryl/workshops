/**
 *  Copyright (C) 2011-2013 Typesafe <http://typesafe.com/>
 */
package com.typesafe.akkademo.service

import akka.actor.{ ActorLogging, Actor }
import com.typesafe.akkademo.common.{ Bet, RetrieveBets }

class BettingService extends Actor with ActorLogging {

  /**
   * TASKS:
   * Create unique sequence/transaction number
   * Create PlayerBet and call betting processor (remotely)
   * Retrieve all bets from betting processor (remotely)
   * Handle timed out transactions (scheduler)
   * Handle registration message from betting processor
   * Handle crash of/unavailable betting processor
   * Keep any message locally until there is a processor service available
   */

  def receive = {
    case bet: Bet     ⇒
    case RetrieveBets ⇒
  }
}
