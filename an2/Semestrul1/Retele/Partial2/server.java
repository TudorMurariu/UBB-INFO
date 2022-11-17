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
		if(args.length < 2) {
			System.out.println("Trebuie sa dati un port si o adresa");
			return;
		}

		int port_actual = Integer.parseInt(args[0]);
		InetAddress ourIPAddress = InetAddress.getByName(args[1]);

        DatagramSocket serverSocket = new DatagramSocket(port_actual, ourIPAddress);
		byte[] receiveData = new byte[1024];
		byte[] sendData = new byte[1024];

		int max = port_actual;
		int countClienti = 0;

		System.out.println("Astept conexiuni: ");
		while(true)
		   {			
			  DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  serverSocket.receive(receivePacket);

			  String nS = new String( receivePacket.getData() );
			  nS = nS.trim();
			  int n = Integer.parseInt(nS);

			  System.out.println("Am ptrimit lungimea : " + nS);
			
			  receivePacket = new DatagramPacket(receiveData, receiveData.length);
			  serverSocket.receive(receivePacket);
			  
			  String sir = new String( receivePacket.getData() );
			  sir = sir.trim();
			  System.out.println("Am primit sirul:\n" + sir);

			  String[] numere = sir.split(" ");

			  int sum = 0;
			  for(int i = 0;i < n;++i)
			  {
					String x  = numere[i];
					if(x.equals(" ") || x.equals(""))
						break;

					int num = Integer.parseInt(x);
					
					while(num != 0)
					{
						if(num % 2 == 1)
							sum += num%10;
						num /= 10;
					}
			  }

			  System.out.println("SUM : " + sum);

			  port_actual = port_actual + sum;
			  if(port_actual > max)
			  	max = port_actual;

			  InetAddress IPAddress = receivePacket.getAddress();
			  int port = receivePacket.getPort();

			  String cielnti_nr = String.valueOf(countClienti);
			  sendData = cielnti_nr.getBytes();
			  
			  DatagramPacket sendPacket =
			  		new DatagramPacket(sendData, sendData.length, IPAddress, port);
			  
			  serverSocket.send(sendPacket);

			  String max_port = String.valueOf(max);
			  sendData = max_port.getBytes();

			  sendPacket =
			  		new DatagramPacket(sendData, sendData.length, IPAddress, port);
			  
			  serverSocket.send(sendPacket);
			  System.out.println("Acum portul maxim este: " + max);

			  countClienti++;
			  serverSocket = new DatagramSocket(port_actual);
			  System.out.println("Am schimbat portul in : " + port_actual);
		   }
      }
}