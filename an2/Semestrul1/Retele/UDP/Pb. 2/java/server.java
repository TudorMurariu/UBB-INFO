/*
Enunt: Clientul trimite serverului doua numere. Serverul le primeste, le afiseaza pe ecran si trimite clientului suma lor.

Compilare:
	javac client.java
	javac server.java
	
Rulare:
	java client
	java server	
*/
import java.io.*;
import java.net.*;

class server
{
   public static void main(String args[]) throws Exception
      {
        DatagramSocket serverSocket = new DatagramSocket(1234);
		byte[] receiveData = new byte[1024];
		byte[] sendData = new byte[1024];
		System.out.println("Astept conexiuni: ");
		while(true)
		   {			   
			  DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  serverSocket.receive(receivePacket);
			  
			  String numarS1 = new String( receivePacket.getData());
			  int numar1 = Integer.parseInt(numarS1.trim());				
			  System.out.println("Am primit: " + numar1);
			  
			  receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  serverSocket.receive(receivePacket);
			  
			  String numarS2 = new String( receivePacket.getData());
			  int numar2 = Integer.parseInt(numarS2.trim());				
			  System.out.println("Am primit: " + numar2);
			
 			  InetAddress IPAddress = receivePacket.getAddress();
			  int port = receivePacket.getPort();
			  Integer rez=numar1+numar2;
			  String bufr=rez.toString();
			  sendData = bufr.getBytes();
			  DatagramPacket sendPacket =
			  new DatagramPacket(sendData, sendData.length, IPAddress, port);
			  serverSocket.send(sendPacket);
		   }
      }
}