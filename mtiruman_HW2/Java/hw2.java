import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Created by raijin on 3/1/16.
 */
public class hw2 {

    public static void hw2(String inFile, String outFile) throws IOException{
        String line = null;
        try{
            FileReader fr = new FileReader(inFile);
            BufferedReader br = new BufferedReader(fr);
            FileWriter fw = new FileWriter(outFile);
            BufferedWriter bw = new BufferedWriter(fw);

            ArrayList stack = new ArrayList();
            while ((line = br.readLine()) != null){
            	   if (line.contains("quit"))
            	   	break;
            	   else
            	   	parse(line, stack);
            }
            for (int i = stack.size() - 1; i >= 0; i--){
                bw.write(stack.get(i) + "\n");
            }
            bw.close();
        }
        catch (FileNotFoundException e){
            e.printStackTrace();
        }
    }

    private static void parse(String command, ArrayList stack){
        if (command.contains(":error:")){
            //System.out.println(command);
            stack.add(":error:");
        }
        else if (command.contains(":true:")) {
            //System.out.println(command);
            stack.add(":true:");
        }
        else if (command.contains(":false:")) {
            //System.out.println(command);
            stack.add(":false:");
        }
        else if (command.contains("neg")) {
            //System.out.println(command);
            if (stack.size() == 0) {
                stack.add(":error:");
            }
            else if (stack.size() > 0){
                if (stack.get(stack.size() - 1) instanceof String) {
                    stack.add(":error:");
                }
                else if (stack.get(stack.size() - 1) instanceof Integer){
                    int a = (-1)*((int) stack.remove(stack.size() - 1));
                    stack.add(a);
                }
            }
        }
        else if (command.contains("push")){
            //String num = command.replaceAll("[^0-9?!\\.]","");
            String num = command.substring(5, command.length());
            if (num.contains("."))
                stack.add(":error:");
            else{
                int a = Integer.parseInt(num);
                stack.add(a);
            }
        }
        else if (command.contains("pop")){
            if (stack.size() == 0) {
                stack.add(":error:");
            }
            else if (stack.size() > 0){
                stack.remove(stack.size() - 1);
            }
        }
        else if (command.contains("add")){
            if (stack.size() <= 1) {
                stack.add(":error:");
            }
            else if (stack.size() > 1){
                int x, y;
                if ((stack.get(stack.size() - 1) instanceof Integer) && (stack.get(stack.size() - 2)) instanceof Integer) {
                    y = (int) stack.remove(stack.size() - 1);
                    x = (int) stack.remove(stack.size() - 1);
                    stack.add(y + x);
                }
                else{
                    stack.add(":error:");
                }
            }
        }
        else if (command.contains("sub")){
            if (stack.size() <= 1) {
                stack.add(":error:");
            }
            else if (stack.size() > 1){
                int x, y;
                if ((stack.get(stack.size() - 1) instanceof Integer) && (stack.get(stack.size() - 2)) instanceof Integer) {
                    y = (int) stack.remove(stack.size() - 1);
                    x = (int) stack.remove(stack.size() - 1);
                    stack.add(x - y);
                }
                else{
                    stack.add(":error:");
                }
            }
        }
        else if (command.contains("mul")){
            if (stack.size() <= 1) {
                stack.add(":error:");
            }
            else if (stack.size() > 1){
                int x, y;
                if ((stack.get(stack.size() - 1) instanceof Integer) && (stack.get(stack.size() - 2)) instanceof Integer) {
                    y = (int) stack.remove(stack.size() - 1);
                    x = (int) stack.remove(stack.size() - 1);
                    stack.add(y*x);
                }
                else{
                    stack.add(":error:");
                }
            }
        }
        else if (command.contains("div")){
            if (stack.size() <= 1) {
                stack.add(":error:");
            }
            else if (stack.size() > 1){
                int x, y;
                if ((stack.get(stack.size() - 1) instanceof Integer) && (stack.get(stack.size() - 2)) instanceof Integer) {
                    y = (int) stack.remove(stack.size() - 1);
                    x = (int) stack.remove(stack.size() - 1);
                    if (y == 0){
                        stack.add(x);
                        stack.add(y);
                        stack.add(":error:");
                    }
                    else {
                        stack.add(x/y);
                    }
                }
                else{
                    stack.add(":error:");
                }
            }
        }
        else if (command.contains("rem")){
            if (stack.size() <= 1) {
                stack.add(":error:");
            }
            else if (stack.size() > 1){
                int x, y;
                if ((stack.get(stack.size() - 1) instanceof Integer) && (stack.get(stack.size() - 2)) instanceof Integer) {
                    y = (int) stack.remove(stack.size() - 1);
                    x = (int) stack.remove(stack.size() - 1);
                    if (y == 0){
                        stack.add(x);
                        stack.add(y);
                        stack.add(":error:");
                    }
                    else {
                        stack.add(x%y);
                    }
                }
                else{
                    stack.add(":error:");
                }
            }
        }
        else if (command.contains("swap")){
            if (stack.size() <= 1) {
                stack.add(":error:");
            }
            else if (stack.size() > 1) {
                Object temp = stack.get(stack.size() - 1);
                stack.set(stack.size() - 1,stack.get(stack.size() - 2));
                stack.set(stack.size() - 2,temp);
            }
        }
    }
}
