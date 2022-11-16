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
        DatagramSocket serverSocket = new DatagramSocket(8893);
		byte[] receiveData = new byte[1024];
		byte[] sendData = new byte[1024];
		System.out.println("Astept conexiuni: ");
		while(true)
		   {			   
			  DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
              serverSocket.receive(receivePacket);
			  
			  String numarS = new String(receivePacket.getData());
			  int numar = Integer.parseInt(numarS.trim());				
			  System.out.println("Am primit: " + numar);

              Boolean prim = true;
              for(int d = 2;d*d <= numar;++d)
                if(numar % d == 0)
                {
                    prim = false;
                    break;
                }

            String rez;
            if(prim)
                rez = "1";
            else
                rez = "0";
                
            sendData = rez.getBytes();

            DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length,receivePacket.getAddress(), receivePacket.getPort());
            serverSocket.send(sendPacket);
		   }
      }
}