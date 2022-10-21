import java.net.*;
import java.io.*;
 
public class Server {
 
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(8081);
    //byte b[] = new byte[100];
  
    while(true) {
      Socket c = s.accept();
      System.out.println("Client connected!");

      DataInputStream socketIn = new DataInputStream(c.getInputStream());
      DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());

      int a = socketIn.readUnsignedShort();
      System.out.println("Am citit a" + a);
      int b = socketIn.readUnsignedShort();
      System.out.println("Am primit de la client numerele: " + a + " " + b);
      System.out.println("Am trimis la server suma lor");

      socketOut.writeShort(a + b);
      socketOut.flush();
  
      c.close();

      if(a == 12345 && b == 7778881)
        break;
    }  
  }
 
}