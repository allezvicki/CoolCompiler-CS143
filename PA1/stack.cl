(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *
 *  Thank god we need not do error checking. All the
 *  input and following behavior would be valid.
 *)

class Stack inherits IO {
    stack : String;

    init() : SELF_TYPE {{
        stack <- "";
        self;
    }};

    push(c : String) : Bool {{
        stack <- stack.concat(c.concat("\n"));
        true;
    }
    };

    pop() : String {
        let i : Int <- stack.length() - 2, continue : Bool <- true, popped : String in
        {
            while continue loop {
                continue <- (1 <= i);
                if continue then
                if stack.substr(i, 1) = "\n" then {
                    i <- i + 1;
                    continue <- false;
                } else i <- i - 1 fi else false fi; 
            } pool;
            popped <- stack.substr(i, stack.length() - i - 1); 
            stack <- stack.substr(0, i);
            popped;
        }
    };

    evaluate_plus() : Object {{
        pop();
        -- let atoi : A2I, int1 : Int <- atoi.a2i(pop()), int2 : Int <- atoi.a2i(pop()) in --
        -- The above is wrong. atoi cannot be used right away, which results in Dispatch to void --
        let int1 : Int <- (new A2I).a2i(pop()), int2 : Int <- (new A2I).a2i(pop()) in
            push((new A2I).i2a(int1 + int2));
    }
    };

    evaluate_s() : Object {{
        pop();
        let s1 : String <- pop(), s2 : String <- pop() in
            stack <- stack.concat(s1).concat("\n").concat(s2).concat("\n");
    }};

    evaluate() : Bool {
        if stack.length() = 0 then true else {
        let len : Int <- stack.length() in
            let c : String <- stack.substr(len - 2, 1) in
                if c = "+" then evaluate_plus() else
                if c = "s" then evaluate_s() else false
                fi fi;
        true;
        } fi
    };

    display() : Bool {{
        out_string(stack);
        true;
    }
    };

    stop() : Bool { false };
    
    operate(c : String) : Bool {
        if c = "+" then
        push(c)
        else if c = "s" then
        push(c)
        else if c = "e" then
        evaluate()
        else if c = "d" then
        display()
        else if c = "x" then
        stop() 
        else push(c) fi fi fi fi fi
    };

    work() : Object {
        let continue : Bool <- true in
        while continue loop {
            out_string("> ");
            let s : String <- in_string() in
            continue <- operate(s);
        } pool
    };
};

class Main inherits IO {

    main() : Object {
        -- (new Stack).init().work() --
        (new Stack).work()
    };

};
