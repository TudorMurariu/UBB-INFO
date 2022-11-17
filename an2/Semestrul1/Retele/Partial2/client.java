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
import java.util.Random;

class client
{
   public static void main(String args[]) throws Exception
   {
      if(args.length < 2) {
			System.out.println("Trebuie sa dati un port si o adresa");
			return;
		}
      
      Random r = new Random();
      final int port = r.nextInt(5000) + 5000;

      int port_srv = Integer.parseInt(args[0]);
      InetAddress IPAddress = InetAddress.getByName(args[1]);

      BufferedReader inFromUser = new BufferedReader(new InputStreamReader(System.in));
      DatagramSocket clientSocket = new DatagramSocket(port ,IPAddress);


      byte[] sendData = new byte[1024];
      byte[] receiveData = new byte[1024];
	  
	   System.out.println("Dati lungimea sirului:");
      String nS = inFromUser.readLine().trim();
      int n = Integer.parseInt(nS);
      StringBuilder stb = new StringBuilder("");

      for(int i = 0;i < n;i++)
      {
         String s = inFromUser.readLine().trim();
         stb.append(s + " ");
      }

      sendData = nS.getBytes();

      DatagramPacket sendPacket = 
         new DatagramPacket(sendData, sendData.length, IPAddress, port_srv);
      clientSocket.send(sendPacket);

      sendData = stb.toString().getBytes();

      sendPacket = 
            new DatagramPacket(sendData, sendData.length, IPAddress, port_srv);
      clientSocket.send(sendPacket);

      System.out.println("Am trimis sirul");
	  
      DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
      clientSocket.receive(receivePacket);
	  
      String numarClienti = new String(receivePacket.getData());
      System.out.println("FROM SERVER NR CLIENTI:" + numarClienti);

      receivePacket = new DatagramPacket(receiveData, receiveData.length);
      clientSocket.receive(receivePacket);

      String max = new String(receivePacket.getData());
      System.out.println("FROM SERVER MAX:" + max);
     
	   clientSocket.close();
   }
}