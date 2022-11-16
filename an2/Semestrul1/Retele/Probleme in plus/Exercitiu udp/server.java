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

class server
{
   public static void main(String args[]) throws Exception
      {
        DatagramSocket serverSocket = new DatagramSocket(6969);
		byte[] receiveData = new byte[1024];
		byte[] sendData = new byte[1024];
		System.out.println("Astept conexiuni: ");
		while(true)
		   {			   
			  DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  for(int i=0;i<1000000000;++i)
              {
                serverSocket.receive(receivePacket);
                System.out.println(new String(receivePacket.getData()));
              }
		   }
      }
}