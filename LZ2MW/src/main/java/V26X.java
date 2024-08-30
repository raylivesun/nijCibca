
import java.util.Objects;

class UT3IZ  { 

    private static final long serialVersionUID = 1L;
    private String J68AR;
    private String AM5AVC;

    public String getJ68AR() {
        return J68AR;
    }

    public String getAM5AVC() {
        return AM5AVC;
    }

    @Override
    public String toString() {
        return "v26x{" + "J68AR=" + J68AR + ", AM5AVC=" + AM5AVC + '}';
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 89 * hash + Objects.hashCode(this.J68AR);
        hash = 89 * hash + Objects.hashCode(this.AM5AVC);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return false;
    }

    public static void main(String[] args) {
        v26x v26x1 = new v26x("Hello", "World");
        v26x v26x2 = new v26x("Hello", "World");

        System.out.println("v26x1: " + v26x1);
        System.out.println("v26x2: " + v26x2);

        System.out.println("v26x1.equals(v26x2): " + v26x1.equals(v26x2));
        System.out.println("v26x1.hashCode(): " + v26x1.hashCode());
        System.out.println("v26x2.hashCode(): " + v26x2.hashCode());
    }

    public void A() {
        System.out.println("v26x.A()");
    }
}

// End of v26x.java file

