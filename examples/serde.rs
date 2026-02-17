#![expect(clippy::unwrap_used, reason = "acceptable in examples")]

use dst_factory::make_dst_factory;
use serde::Serialize;

/// A log entry with a fixed severity level and a variable-length message.
#[derive(Serialize)]
#[make_dst_factory(deserialize)]
struct LogEntry {
    severity: u8,
    message: str,
}

/// A sensor reading with a fixed timestamp and a variable-length list of samples.
#[derive(Serialize)]
#[make_dst_factory(deserialize)]
struct SensorReading {
    timestamp: u64,
    samples: [f32],
}

fn main() {
    // --- str tail ---

    let entry: Box<LogEntry> = LogEntry::build(3, "disk usage at 90%");
    println!("Original:     severity={}, message={:?}", entry.severity, &entry.message);

    // Serialize to JSON
    let json = serde_json::to_string(&*entry).unwrap();
    println!("Serialized:   {json}");

    // Deserialize back into a Box<LogEntry>
    let restored: Box<LogEntry> = serde_json::from_str(&json).unwrap();
    println!("Deserialized: severity={}, message={:?}", restored.severity, &restored.message);

    assert_eq!(entry.severity, restored.severity);
    assert_eq!(&entry.message, &restored.message);

    // --- slice tail ---

    let reading: Box<SensorReading> = SensorReading::build_from_slice(1_700_000_000, &[23.1, 23.4, 22.9, 23.0]);
    println!("\nOriginal:     timestamp={}, samples={:?}", reading.timestamp, &reading.samples);

    let json = serde_json::to_string(&*reading).unwrap();
    println!("Serialized:   {json}");

    let restored: Box<SensorReading> = serde_json::from_str(&json).unwrap();
    println!("Deserialized: timestamp={}, samples={:?}", restored.timestamp, &restored.samples);

    assert_eq!(reading.timestamp, restored.timestamp);
    assert_eq!(&reading.samples, &restored.samples);
}
