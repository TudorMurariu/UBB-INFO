/*
Enunt: Clientul trimite serverului un numar. Serverul il primeste si il afiseaza pe ecran.

Compilare:
	javac client.java
	javac server.java
	
Rulare:
	java client
	java server	
*/
import java.io.*;
import java.net.*;

class client
{
   public static void main(String args[]) throws Exception
   {
      //BufferedReader inFromUser = new BufferedReader(new InputStreamReader(System.in));
      DatagramSocket clientSocket = new DatagramSocket();
      InetAddress IPAddress = InetAddress.getByName("localhost");
      byte[] sendData = new byte[1024];
	  DatagramPacket sendPacket;

      for(int i =0;i<1000000000;++i) {
        sendData = String.valueOf(i).getBytes();
        sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress, 6969);
        clientSocket.send(sendPacket);
      }
	  
	  clientSocket.close();
   }
}