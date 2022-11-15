import java.net.*;
import java.io.*;
 
public class Client{
 
    private static final String SERVER_ADDRESS = "127.0.0.1";
    private static final int SERVER_PORT = 1234;
    private static final int UNSIGNED_SHORT_MAX_VALUE = 65535;
    private static final int UNSIGNED_SHORT_MIN_VALUE = 0;
 
    public static void main(String args[]) {
        Socket socket = null;
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(System.in));
            socket = new Socket(SERVER_ADDRESS, SERVER_PORT);
            
            String m=readString("sir=",reader);
            int a = readUnsignedShortA("lungime = ", reader);
            int b = readUnsignedShortA("pozitie = ", reader);
            
			writeStringToSocket(m,socket);
            
            writeIntegersToSocket(a, b, socket);
            int lb = readUnsignedShortSocket(socket);
			System.out.println("Am primit lb= "+lb);

			readStringFromSocket(lb,socket);
         
        } catch (IOException e) {
            System.err.println("Caught exception " + e.getMessage());
        } finally {
            closeStreams(socket,reader);
        }
    }
 
    private static void readStringFromSocket(int lb, Socket clientSocket) throws IOException {
		byte[] messageByte = new byte[1000];
		String messageString="";
		DataInputStream in = new DataInputStream(clientSocket.getInputStream());
		messageString += new String(messageByte, 0, in.read(messageByte));
		System.out.println("Am primit: "+messageString);
    }
 
    private static void writeIntegersToSocket(int a, int b, Socket c) throws IOException {
        DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());
        socketOut.writeShort(a);
		System.out.println("Am trimis "+a);
        socketOut.writeShort(b);
		System.out.println("Am trimis "+b);
        socketOut.flush();
    }
 
    private static void writeStringToSocket(String m,Socket c)throws IOException{
    	 DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());    	 
    	 socketOut.writeShort(m.length());
    	 System.out.println("Am trimis "+m.length());
    	 socketOut.writeBytes(m);
		 System.out.println("Am trimis "+m);
    }
    
    private static int readUnsignedShortSocket(Socket c) throws IOException {        
		DataInputStream socketIn = new DataInputStream(c.getInputStream());
		int unsignedShortNumber = socketIn.readUnsignedShort();		
        return unsignedShortNumber;
    }    
    
    private static int readUnsignedShortA(String message, BufferedReader reader) throws IOException {
        int unsignedShortNumber = 0;
        System.out.print(message);
        try {
            unsignedShortNumber = Integer.parseInt(reader.readLine());
            if (unsignedShortNumber < UNSIGNED_SHORT_MIN_VALUE || unsignedShortNumber > UNSIGNED_SHORT_MAX_VALUE) {
                throw new IllegalArgumentException("The given number must be unsigned short [0..65535]!");
            }
        } catch (NumberFormatException e) {
            System.err.println("The given input is not an integer!");
        }
        return unsignedShortNumber;
    }    
    
    private static String readString(String message,BufferedReader reader) throws IOException{
    	String str = null;    	
    	System.out.print(message);
    	try{	
    		str=reader.readLine();
    	}catch (IOException e) {
            System.err.println("The given input is not a string");
            }
    	return str;    	
    }    
 
    private static void closeStreams(Socket socket, BufferedReader reader) {
        if (socket != null) {
            try {
                socket.close();
            } catch (IOException e) {
                System.err.println("Could not close socket!");
            }
        }
        if (reader != null) {
            try {
                reader.close();
            } catch (IOException e) {
                System.err.println("Could not close reader!");
            }
        }
    }
 
}