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

class server
{
   public static void main(String args[]) throws Exception
      {
        DatagramSocket serverSocket = new DatagramSocket(1234);
		
		System.out.println("Astept conexiuni: ");
		while(true)
		   {			   
			  Integer rez=0;
			  byte[] receiveData = new byte[1024];
			  byte[] sendData = new byte[1024];
			  
			  DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  serverSocket.receive(receivePacket);
			  
			  String lungimeS = "";
			  lungimeS=new String( receivePacket.getData());
			  int lungime = Integer.parseInt(lungimeS.trim());				
			  System.out.println("Am primit lungimea: " + lungime);
			  
			  for(int i=0;i<lungime;i++){
				  receiveData = new byte[1024];
			  receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  serverSocket.receive(receivePacket);
			  
			  String numarS2 = new String( receivePacket.getData());
			  int numar2 = Integer.parseInt(numarS2.trim());				
			  System.out.println("Am primit: " + numar2);
			  rez=rez+numar2;
			  }
			
 			  InetAddress IPAddress = receivePacket.getAddress();
			  int port = receivePacket.getPort();
			  
			  String bufr=rez.toString();
			  System.out.println("Am trimis: " + bufr);
			  sendData = bufr.getBytes();

			  DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress, port);
			  serverSocket.send(sendPacket);
		   }
      }
}