package org.kissweb;

import static org.kissweb.CL.*;

/**
 *
 * Class that demonstrates the use of the CL class.
 *
 * To run:  java -cp Kiss.jar org.kissweb.CLExample
 *
 * Author: Blake McBride
 * Date: 1/5/22
 */
public class CLExample {

    private static int NAME = CL.Questions.add(CLExample::getName);
    private static int AGE = CL.Questions.add(CLExample::getAge);
    private static int YEAR = CL.Questions.add(CLExample::getDate);
    private static int SEX = CL.Questions.add(CLExample::getSex);
    private static int COLOR = CL.Questions.add(CLExample::getColor);

    private static String name;
    private static int age;
    private static int year;
    private static char sex;
    private static String color;

    /**
     * Main entry point demonstrating command line question functionality.
     * 
     * @param args command line arguments
     */
    public static void main(String [] args) {
        boolean r = CL.Questions.run();
        if (r)
            println(name + " " + age + " " + year + " " + sex + " " + color);
        else
            println("aborted");
    }

    private static int getName() {
        while (true) {
            name = inputString(1, 30, "What is your name");
            switch (EHN(name)) {
                case END:
                    return PREVIOUS;
                case HELP:
                    println("The name help text");
                    break;
                case NOTHING:
                    break;
                case VALUE:
                    return NEXT;
            }
        }
    }

    private static int getAge() {
        String res;
        while (true) {
            res = inputNumber(10, 110, 0, "What is your age");
            switch (EHN(res)) {
                case END:
                    return PREVIOUS;
                case HELP:
                    println("The age help text");
                    break;
                case NOTHING:
                    break;
                case VALUE:
                    age = (int) NumberUtils.parseLong(res);
                    return NEXT;
            }
        }
    }

    private static int getDate() {
        String res;
        while (true) {
            res = inputDate(19000101, 21991231, "What date did you graduate high school");
            switch (EHN(res)) {
                case END:
                    return PREVIOUS;
                case HELP:
                    println("The year help text");
                    break;
                case NOTHING:
                    break;
                case VALUE:
                    year = NumberUtils.parseInt(res);
                    return NEXT;
            }
        }
    }

    private static int getSex() {
        String res;
        while (true) {
            res = inputCharacter("mf", "What is your sex");
            switch (EHN(res)) {
                case END:
                    return PREVIOUS;
                case HELP:
                    println("The sex help text");
                    break;
                case NOTHING:
                    break;
                case VALUE:
                    sex = res.charAt(0);
                    return NEXT;
            }
        }
    }

    private static int getColor() {
        String res;
        while (true) {
            res = inputList("red,blue,orange,brown", "What is your favorite color");
            switch (EHN(res)) {
                case END:
                    return PREVIOUS;
                case HELP:
                    println("The color help text");
                    break;
                case NOTHING:
                    break;
                case VALUE:
                    color = res;
                    return NEXT;
            }
        }
    }
}
