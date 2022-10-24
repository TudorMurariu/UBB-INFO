import java.net.*;
import java.io.*;
 
public class Server {
 
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(7072);
    //byte b[] = new byte[100];
  
    while(true) {
      Socket c = s.accept();
      System.out.println("Client connected!");

      DataInputStream socketIn = new DataInputStream(c.getInputStream());
      DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());

      //int a = socketIn.readUnsignedShort();
      byte[] a_byte = new byte[4];
      byte[] b_byte = new byte[4];
      socketIn.read(a_byte);
      //System.out.println("Am citit a" + a);
      //int b = socketIn.readUnsignedShort();
      socketIn.read(b_byte);
      
      int a = (a_byte[0]<<0) & 0xff000000|
      (a_byte[1]<<8) & 0x00ff0000|
      (a_byte[2]<< 16) & 0x0000ff00|
      (a_byte[3]<< 24) & 0x000000ff;

      int b = (b_byte[0]<<0) & 0xff000000|
      (b_byte[1]<<8) & 0x00ff0000|
      (b_byte[2]<< 16) & 0x0000ff00|
      (b_byte[3]<< 24) & 0x000000ff;

      System.out.println("Am primit de la client numerele: " + a + " " + b);
      System.out.println("Am trimis la server suma lor " + (a + b));

      //socketOut.writeShort(a + b);
      socketOut.write(a + b);
      socketOut.flush();

      if(a == 4)
        break;
      
      c.close();
    }  

    s.close();
  }
}