/*
Enunt: Clientul trimite serverului un sir de numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.

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
      byte[] sendData = new byte[1024];
      byte[] receiveData = new byte[1024];
	  
	  System.out.println("Dati lungimea sirului:");
      String lungime = inFromUser.readLine().trim();
      sendData = lungime.getBytes();
	  System.out.println("Am trimis lungimea: " + lungime);
	  
      DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress, 1234);
      clientSocket.send(sendPacket);
	  
	  for(int i=0;i<Integer.parseInt(lungime);i++){
		  System.out.println("Dati sir["+i+"]:");
		  String numar2 = inFromUser.readLine().trim();
		  sendData = numar2.getBytes();
		  System.out.println("Am trimis: " + numar2);
		  sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress, 1234);
		  clientSocket.send(sendPacket);
	  }
	  
      DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
      clientSocket.receive(receivePacket);
	  
      String rez = new String(receivePacket.getData());
      System.out.println("FROM SERVER:" + rez);
     
	  clientSocket.close();
   }
}