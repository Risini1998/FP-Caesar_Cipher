package Assignments
import util.control.Breaks._

object CaesarCipher {
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def Encryption() = {
    val inputText = scala.io.StdIn.readLine("Secret Message: ")
    val shift = (scala.io.StdIn.readLine("Shift By: ").toInt + alphabet.size) % alphabet.size

    val outputText = inputText.map((c: Char) => {
      val x = alphabet.indexOf(c.toUpper)
      if (x == -1) {
        c
      }
      else {
        alphabet((x + shift) % alphabet.size)
      }
    });

    println("Encrypted Message: " + outputText)
  }

  def Decryption() = {
    val inputText = scala.io.StdIn.readLine("Encrypted Message: ")
    val shift = (scala.io.StdIn.readLine("Shifted By: ").toInt + alphabet.size) % alphabet.size

    val outputText = inputText.map((c: Char) => {
      val x = alphabet.indexOf(c.toUpper)
      if (x == -1) {
        c
      }
      else {
        alphabet((x - shift + alphabet.size) % alphabet.size)
      }
    });

    println("Secret Message: " + outputText)
  }

  def Cipher(func:() => Unit) = {
    func()
  }

  def main(args: Array[String]): Unit = {

    breakable {
      do {
        print("\n1. Encrypt \n2. Decrypt \n")
        print("Enter option: ")
        val op = scala.io.StdIn.readInt()

        op match {
          case 1 => Cipher(Encryption)
          case 2 => Cipher(Decryption)
          case _ => break
        }
      } while (true)
    }
  }
}
