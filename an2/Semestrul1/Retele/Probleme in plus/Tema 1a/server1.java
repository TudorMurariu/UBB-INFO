import java.net.*;
import java.io.*;
 
public class server {
 
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(12347);
    //byte b[] = new byte[100];
  
    while(true) {
      Socket c = s.accept();
      System.out.println("Client connected!");

      DataInputStream socketIn = new DataInputStream(c.getInputStream());
      DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());

      int a = socketIn.readInt();
      //System.out.println("Am citit a" + a);
      int b = socketIn.readInt();
      System.out.println("Am primit de la client numerele: " + a + " " + b);
      System.out.println("Am trimis la server suma lor");

      socketOut.writeInt(a + b);
      socketOut.flush();

      if(a == 12432 && b == 763434)
        break;
      
      c.close();
    }  

    s.close();
  }
}