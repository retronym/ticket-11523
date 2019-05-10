package example;

public class Client {
    class D extends example.AbstractPartialFunction<String, String> {
        public boolean isDefinedAt(String s) { return false; }
    };
}
