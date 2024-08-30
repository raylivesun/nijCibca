package netbeans.lz2mw;



class UT3IZE  {
    private String name;
    private int age;

    public UT3IZE(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public int getAge() {
        return age;
    }

    public String describeConstable() {
        return "Class: " + getClass().getSimpleName() + ", Name: " + name + ", Age: " + age;
    }
    
}