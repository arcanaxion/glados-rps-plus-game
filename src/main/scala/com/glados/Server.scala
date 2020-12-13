package com.glados

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.receptionist.{Receptionist,ServiceKey}
import akka.actor.typed.scaladsl.adapter._
import akka.management.cluster.bootstrap.ClusterBootstrap
import akka.management.scaladsl.AkkaManagement
import akka.discovery.Discovery
import akka.cluster.typed._
import com.typesafe.config.ConfigFactory
import com.glados.protocol.JsonSerializable
import scalafx.collections.ObservableHashSet


object ChatServer {
    sealed trait Command extends JsonSerializable

    // chat protocol 
    case class JoinChat(name: String, from: ActorRef[ChatClient.Command]) extends Command
    case class Leave(name: String, from: ActorRef[ChatClient.Command]) extends Command

    val ServerKey: ServiceKey[ChatServer.Command] = ServiceKey("ChatServer")
    val members = new ObservableHashSet[User]()

    members.onChange{(ns, _) =>
        for(member <- ns){
            member.ref ! ChatClient.MemberList(members.toList)
        }
    }

    def apply(): Behavior[ChatServer.Command] = Behaviors.setup { context =>

        context.system.receptionist ! Receptionist.Register(ServerKey, context.self)
        
        Upnp.bindPort(context)

        Behaviors.receiveMessage { message =>
            message match {
                case JoinChat(name, from) =>
                    members += User(name, from)
                    from ! ChatClient.Joined(members.toList)
                    Behaviors.same
                case Leave(name, from) => 
                    members -= User(name, from)
                    Behaviors.same
            }
        }
    }
}

object Server extends App {
    val config = ConfigFactory.load()
    val mainSystem = akka.actor.ActorSystem("HelloSystem", MyConfiguration.askDevConfig().withFallback(config))
    val typedSystem: ActorSystem[Nothing] = mainSystem.toTyped
    val cluster = Cluster(typedSystem)
    cluster.manager ! Join(cluster.selfMember.address)
    AkkaManagement(mainSystem).start()
    //val serviceDiscovery = Discovery(mainSystem).discovery
    ClusterBootstrap(mainSystem).start() 
    //val greeterMain: ActorSystem[ChatServer.Command] = ActorSystem(ChatServer(), "HelloSystem")
    mainSystem.spawn(ChatServer(), "ChatServer")
}
