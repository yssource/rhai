use rhai::{Engine, RegisterFn, INT};

fn main() {
    // Channel: Script -> Master
    let (tx_script, rx_master) = std::sync::mpsc::channel();
    // Channel: Master -> Script
    let (tx_master, rx_script) = std::sync::mpsc::channel();

    // Spawn thread with Engine
    std::thread::spawn(move || {
        // Create Engine
        let mut engine = Engine::new();

        // Register API
        // Notice that the API functions are blocking
        engine
            .register_fn("get", move || rx_script.recv().unwrap())
            .register_fn("put", move |v: INT| tx_script.send(v).unwrap());

        // Run script
        engine
            .consume(
                r#"
                    print("Starting script loop...");

                    loop {
                        let x = get();
                        print("Script Read: " + x);
                        x += 1;
                        print("Script Write: " + x);
                        put(x);
                    }
                "#,
            )
            .unwrap();
    });

    // This is the main processing thread

    println!("Starting main loop...");

    let mut value: INT = 0;

    while value < 10 {
        println!("Value: {}", value);
        // Send value to script
        tx_master.send(value).unwrap();
        // Receive value from script
        value = rx_master.recv().unwrap();
    }
}
