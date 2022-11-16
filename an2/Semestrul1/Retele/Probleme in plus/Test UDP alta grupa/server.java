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
import java.util.HashSet;
import java.util.Set;

class server
{
   public static void main(String args[]) throws Exception
      {
        DatagramSocket serverSocket = new DatagramSocket(8801);
        byte[] receiveData = new byte[1024];
		System.out.println("Astept conexiuni: ");
		while(true)
		{		
            DatagramPacket recivePacket = new DatagramPacket(receiveData, receiveData.length);
            serverSocket.receive(recivePacket);

            ServerThred t = new ServerThred(serverSocket, recivePacket, receiveData);
            t.start();
		}
        //serverSocket.close();
      }
}

class ServerThred extends Thread {
    private DatagramSocket serverSocket;
    private DatagramPacket recivePacket;
    byte[] receiveData;
    private static int iterationLevel = 0;

    public ServerThred(DatagramSocket serverSocket, DatagramPacket recivePacket, byte[] receiveData) {
        this.serverSocket = serverSocket;
        this.recivePacket = recivePacket;
        this.receiveData = receiveData;
    }

    @Override
    public void run() {
        try {
            System.out.println("Am inceput clientul cu numarul: " + iterationLevel);
		    byte[] sendData = new byte[1024];

            String sir = new String(receiveData);
            System.out.println("Am primit: " + sir + "\nde la " + iterationLevel);

            sir = sir.trim();
            int whitespace_count = 0;
            Set<Character> set = new HashSet<>();
            for(int i=0;i<sir.length();++i)
            {
                if(sir.charAt(i) == ' ')
                    ++whitespace_count;
                else
                    set.add(sir.charAt(i));
            }

            System.out.println(recivePacket.getPort());
            String port = String.valueOf(recivePacket.getPort() + whitespace_count);
            sendData = port.getBytes();

            DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, recivePacket.getAddress(), recivePacket.getPort());
            serverSocket.send(sendPacket);

            StringBuilder stb = new StringBuilder("");

            for(Character x : set) {
                String litera = String.valueOf(x);

                stb.append(litera + " ");
                System.out.print(x + " ");
            }

            System.out.println();

            String litere = stb.toString();
            sendData = litere.getBytes();

            sendPacket = new DatagramPacket(sendData, sendData.length, recivePacket.getAddress(), recivePacket.getPort());
            serverSocket.send(sendPacket);

            String ip = String.valueOf(recivePacket.getAddress());
            sendData = ip.getBytes();

            sendPacket = new DatagramPacket(sendData, sendData.length, recivePacket.getAddress(), recivePacket.getPort());
            serverSocket.send(sendPacket);

            iterationLevel++;
        } 
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}