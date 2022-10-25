import java.net.*;
import java.io.*;
 
public class server1 {
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(7070);
  
    while(true) {
      Socket c = s.accept();
      System.out.println("Client connected!");

      DataInputStream socketIn = new DataInputStream(c.getInputStream());
      DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());

      int n = socketIn.readInt();
      System.out.println("n = " + n);
      int sum = 0, x;
      for(int i=0;i<n;++i)
      {
        x = socketIn.readInt();
        sum += x;
        System.out.println("x: " + x);
      }
      
      socketOut.writeInt(sum);
      System.out.println("sum = " + sum);
      socketOut.flush();
      
      c.close();

      if(n == 0)
        break;
    }  

    s.close();
  }
}