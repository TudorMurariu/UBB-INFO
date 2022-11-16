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
        DatagramSocket serverSocket = new DatagramSocket(8880);
		byte[] receiveData1 = new byte[1024];
        byte[] receiveData2 = new byte[1024];
		byte[] sendData = new byte[1024];
		System.out.println("Astept conexiuni: ");
		while(true)
		   {			   
			  DatagramPacket receivePacket1 = new DatagramPacket(receiveData1, receiveData1.length);
			  DatagramPacket receivePacket2 = new DatagramPacket(receiveData1, receiveData1.length);
              serverSocket.receive(receivePacket1);
              serverSocket.receive(receivePacket2);
			  
			  String numarS1 = new String(receivePacket1.getData());
			  int numar1 = Integer.parseInt(numarS1.trim());				
			  System.out.println("Am primit: " + numar1);

              String numarS2 = new String(receivePacket2.getData());
			  int numar2 = Integer.parseInt(numarS2.trim());				
			  System.out.println("Am primit: " + numar2);

              int sum = numar1 + numar2;
              String sumS = String.valueOf(sum);
              sendData = sumS.getBytes();
              System.out.println("Am trimis: " + sumS);
              DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length,receivePacket1.getAddress(), receivePacket1.getPort());
              serverSocket.send(sendPacket);
		   }
      }
}