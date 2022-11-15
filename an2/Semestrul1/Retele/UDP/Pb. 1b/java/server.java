/*
Enunt: Clientul trimite serverului un numar. Serverul il primeste, il afiseaza pe ecran si trimite clientului dublul numarului.

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
			  
			  String numarS = new String( receivePacket.getData());
			  int numar = Integer.parseInt(numarS.trim());				
			  System.out.println("Am primit: " + numar);
			
 			  InetAddress IPAddress = receivePacket.getAddress();
			  int port = receivePacket.getPort();
			  Integer rez=numar*2;
			  String bufr=rez.toString();
			  sendData = bufr.getBytes();
			  DatagramPacket sendPacket =
			  new DatagramPacket(sendData, sendData.length, IPAddress, port);
			  serverSocket.send(sendPacket);
		   }
      }
}