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
      BufferedReader inFromUser = new BufferedReader(new InputStreamReader(System.in));
      DatagramSocket clientSocket = new DatagramSocket();
      InetAddress IPAddress = InetAddress.getByName("localhost");
      byte[] sendData1 = new byte[1024];
      byte[] sendData2 = new byte[1024];
      byte[] receiveData = new byte[1024];
	  
	   System.out.println("Dati un numar:");
      String numar1 = inFromUser.readLine().trim();
      sendData1 = numar1.getBytes();
	   System.out.println("Am trimis: " + numar1);

      System.out.println("Dati un numar:");
      String numar2 = inFromUser.readLine().trim();
      sendData2 = numar2.getBytes();
	   System.out.println("Am trimis: " + numar2);
	  
      DatagramPacket sendPacket1 = new DatagramPacket(sendData1, sendData1.length, IPAddress, 8880);
      DatagramPacket sendPacket2 = new DatagramPacket(sendData2, sendData2.length, IPAddress, 8880);
      clientSocket.send(sendPacket1);
      clientSocket.send(sendPacket2);

      DatagramPacket recivePacket = new DatagramPacket(receiveData, receiveData.length);
      clientSocket.receive(recivePacket);

      String recv =  new String(receiveData);

      System.out.println("Am primit: " + recv);
	  
	  clientSocket.close();
   }
}