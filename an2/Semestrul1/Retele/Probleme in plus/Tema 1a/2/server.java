import java.net.*;
import java.io.*;
 
public class server {
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(7099);
  
    while(true) 
    {
        Socket c = s.accept();
        System.out.println("Client connected!");

        DataInputStream socketIn = new DataInputStream(c.getInputStream());
        DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());

       int n = socketIn.readInt();
       System.out.println("Am primit numarul: " + n);

        byte sir_in_bytes[] = new byte[n];
        socketIn.read(sir_in_bytes, 0, n);

        String sir = new String(sir_in_bytes);
        System.out.println("Am primit sirul: " + sir);
        int number_of_spaces = 0;

        for(int i = 0;i < n; ++i)
          if(sir.charAt(i) == ' ')
            ++number_of_spaces;

        System.out.println("Am trimis numarul de spatii :" + number_of_spaces); 
        socketOut.writeInt(number_of_spaces);
        socketOut.flush();
      
        c.close();
        
        break;
    }  

    s.close();
  }
}