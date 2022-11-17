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
    byte[] sendData = new byte[1024];
    byte[] receiveData = new byte[1024];
	  
	System.out.println("Dati un sir de cuvinte:");
    String sir = inFromUser.readLine();
    sendData = sir.getBytes();
	System.out.println("Am trimis: " + sir);

    DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress, 8803);
    clientSocket.send(sendPacket);


    DatagramPacket recivePacket = new DatagramPacket(receiveData, receiveData.length);
    clientSocket.receive(recivePacket);

    String port = new String(recivePacket.getData());
    System.out.println("Am primit: " + port);

    recivePacket = new DatagramPacket(receiveData, receiveData.length);
    clientSocket.receive(recivePacket);

    String litere = new String(recivePacket.getData());
    System.out.println(litere);

    recivePacket = new DatagramPacket(receiveData, receiveData.length);
    clientSocket.receive(recivePacket);

    String ip_rez = new String(recivePacket.getData());
    System.out.println("Am primit: " + ip_rez);
	  
	clientSocket.close();
   }
}