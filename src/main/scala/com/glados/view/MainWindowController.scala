package com.glados.view
import akka.actor.typed.ActorRef
import scalafxml.core.macros.sfxml
import scalafx.event.ActionEvent
import scalafx.scene.control.{Alert, ButtonType, DialogPane, Label, ListView, TextField}
import scalafx.scene.control.Alert.AlertType
import com.glados.ChatClient
import com.glados.ChatClient.getClass
import com.glados.User
import com.glados.Client
import com.glados.Client.loader
import javafx.fxml

import scalafx.collections.ObservableBuffer
import scalafx.Includes._

@sfxml
class MainWindowController(private val txtName: TextField,
private val lblStatus: Label, private val choiceDisplay: Label, private val listUser: ListView[User],

private val txtMessage: TextField) {
    var chatClientRef: Option[ActorRef[ChatClient.Command]] = None

    def handleJoin(action: ActionEvent): Unit = {
        if(txtName != null)
          chatClientRef map (_ ! ChatClient.StartJoin(txtName.text()))
    }

    def displayStatus(text: String): Unit = {
        lblStatus.text = text
    }

    def displayChoice(text: String): Unit = {
        choiceDisplay.text = text
    }

  def updateList(x: Iterable[User]): Unit ={
    listUser.items = new ObservableBuffer[User]() ++= x
  }

  def rejectAlert(): Unit = {
    new Alert(AlertType.Information) {
      title = "User: " + txtName.getText()
      headerText = "Rejected!"
      contentText = "The user you have selected is currently ingame or has rejected your challenge"
    }.showAndWait()
    ChatClient.challengeStatus = "None"
  }

  def challengeAlert(){
  //   Create and show confirmation alert
    val alert = new Alert(AlertType.Confirmation) {
      title = "User: " + txtName.getText()
      headerText = "You have received a challenge from User."
      contentText = "Do you wish to accept this challange?"
    }

    val result = alert.showAndWait()

    // React to user's selectioon
    result match {
      case Some(ButtonType.OK) => ChatClient.challengeDecision = "yes"
      case _                    => ChatClient.challengeDecision = "no"
    }
  }

  def handleChallenge(action: ActionEvent): Unit = {
    if (listUser.selectionModel().selectedIndex.value >= 0) {
      Client.userRef ! ChatClient.SendChallenge(listUser.selectionModel().selectedItem.value.ref)
    }
  }

  def startGameAlert(): String ={
    val ButtonTypeRock = new ButtonType("Rock")
    val ButtonTypePaper = new ButtonType("Paper")
    val ButtonTypeScissors = new ButtonType("Scissors")

    val alert = new Alert(AlertType.Confirmation) {
      title = "User: " + txtName.getText()
      headerText = "You have accepted the challenge."
      contentText = "Choose your option."
      buttonTypes = Seq(        ButtonTypeRock, ButtonTypePaper, ButtonTypeScissors, ButtonType.Cancel)
    }

    val result = alert.showAndWait()

    result match {
      case Some(ButtonTypeRock)   => displayChoice("rock")
                                     return "rock"

      case Some(ButtonTypePaper)   => displayChoice("paper")
                                      return "paper"

      case Some(ButtonTypeScissors) => displayChoice("scissors")
                                       return "scissors"
      case _ => return "reject"
    }
  }

  def endGameAlert(opponentChoice: String): Unit = {
    var selfChoice = new String
    var gameResult = new String
    var opponentResult = new String
    val ButtonTypeRock = new ButtonType("Rock")
    val ButtonTypePaper = new ButtonType("Paper")
    val ButtonTypeScissors = new ButtonType("Scissors")

    val alert = new Alert(AlertType.Confirmation) {
      title = "User: " + txtName.getText()
      headerText = "Your opponent has chosen."
      contentText = "Choose your option."
      buttonTypes = Seq(ButtonTypeRock, ButtonTypePaper, ButtonTypeScissors, ButtonType.Cancel)
    }

    val result = alert.showAndWait()

    result match {
      case Some(ButtonTypeRock) => selfChoice = "rock"
                                   displayChoice("rock")
      case Some(ButtonTypePaper) => selfChoice = "paper"
                                    displayChoice("paper")
      case Some(ButtonTypeScissors) => selfChoice = "scissors"
                                       displayChoice("scissors")
      case _ => Client.userRef ! ChatClient.SendReject(listUser.selectionModel().selectedItem.value.ref)
    }

    if (selfChoice == "rock") {
      opponentChoice match {
        case "rock" => gameResult = "draw"
        case "paper" => gameResult = "lose"
        case "scissors" => gameResult = "win"
      }
    }
    if (selfChoice == "paper") {
      opponentChoice match {
        case "rock" => gameResult = "win"
        case "paper" => gameResult = "draw"
        case "scissors" => gameResult = "lose"
      }
    }
    if (selfChoice == "scissors") {
      opponentChoice match {
        case "rock" => gameResult = "lose"
        case "paper" => gameResult = "win"
        case "scissors" => gameResult = "draw"
      }
    }

    gameResult match {
      case "win" => opponentResult = "lose"
      case "lose" => opponentResult = "win"
      case "draw" => opponentResult = "draw"
    }

    Client.userRef ! ChatClient.SendResult(listUser.selectionModel().selectedItem.value.ref, opponentChoice, selfChoice, opponentResult)
    displayResult(selfChoice, opponentChoice, gameResult)

  }

  def displayResult(selfChoice: String, opponentChoice: String, gameResult: String): Unit ={
    new Alert(AlertType.Information) {
      title = "User: " + txtName.getText()
      headerText = "You chose " + selfChoice + " while your opponent chose " + opponentChoice
      contentText = "You " + gameResult
    }.showAndWait()
    ChatClient.challengeStatus = "None"
    displayChoice("None")
  }

}