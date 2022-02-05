package org.kissweb;

import static org.kissweb.CL.*;

/**
 *
 * Class that demonstrates the use of the CL class.
 *
 * Author: Blake McBride
 * Date: 1/5/22
 */
public class CLExample {

    private static final int START = 1000;
    private static final int NAME = START;
    private static final int AGE = START + 1;
    private static final int YEAR = START + 2;
    private static final int SEX = START + 3;
    private static final int COLOR = START + 4;

    private String name;
    private int age;
    private int year;
    private char sex;
    private String color;

    public static void main(String [] args) {
        new CLExample().main();
    }

    private void main() {
        Questions q = new Questions(START);
        q.add(NAME, this::getName);
        q.add(AGE, this::getAge);
        q.add(YEAR, this::getDate);
        q.add(SEX, this::getSex);
        q.add(COLOR, this::getColor);
        boolean r = q.run();
        if (r)
            println(name + " " + age + " " + year + " " + sex + " " + color);
        else
            println("aborted");
    }

    private int getName() {
        while (true) {
            name = inputString(1, 30, "What is your name");
            switch (EHN(name)) {
                case 1:  // end
                    return -1;
                case 2: // help
                    println("The name help text");
                    continue;
                case 3:  // nothing
                    continue;
            }
            break;
        }
        return 1;
    }

    private int getAge() {
        String res;
        while (true) {
            res = inputNumber(10, 110, 0, "What is your age");
            switch (EHN(res)) {
                case 1:  // end
                    return -1;
                case 2: // help
                    println("The age help text");
                    continue;
                case 3:  // nothing
                    continue;
            }
            break;
        }
        age = (int) NumberUtils.parseLong(res);
        return 1;
    }

    private int getDate() {
        String res;
        while (true) {
            res = inputDate(19000101, 21991231, "What date did you graduate high school");
            switch (EHN(res)) {
                case 1:  // end
                    return -1;
                case 2: // help
                    println("The year help text");
                    continue;
                case 3:  // nothing
                    continue;
            }
            break;
        }
        year = NumberUtils.parseInt(res);
        return 1;
    }

    private int getSex() {
        String res;
        while (true) {
            res = inputCharacter("mf", "What is your sex");
            switch (EHN(res)) {
                case 1:  // end
                    return -1;
                case 2: // help
                    println("The sex help text");
                    continue;
                case 3:  // nothing
                    continue;
            }
            break;
        }
        sex = res.charAt(0);
        return 1;
    }

    private int getColor() {
        String res;
        while (true) {
            res = inputList("red,blue,orange,brown", "What is your favorite color");
            switch (EHN(res)) {
                case 1:  // end
                    return -1;
                case 2: // help
                    println("The color help text");
                    continue;
                case 3:  // nothing
                    continue;
            }
            break;
        }
        color = res;
        return 1;
    }
}
