package com.glados
import com.glados.protocol.JsonSerializable
import akka.actor.typed.ActorRef

case class User(name: String, ref: ActorRef[RPSClient.Command]) extends JsonSerializable {
    override def toString: String = {
        name
    }
}
