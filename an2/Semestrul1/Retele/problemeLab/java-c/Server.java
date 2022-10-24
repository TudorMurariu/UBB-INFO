import java.net.*;
import java.io.*;
 
public class Server {
 
public static void main(String args[]) throws Exception {
  ServerSocket s = new ServerSocket(1234);
  byte b[] = new byte[100];
 
  while(true) {
    Socket c = s.accept();
    System.out.println("Client connected!");
    c.getInputStream().read(b);
    System.out.println(new String(b));
 
    c.close();
  }  
}
 
}