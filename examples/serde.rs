#![expect(clippy::unwrap_used, reason = "acceptable in examples")]

use std::rc::Rc;
use std::sync::Arc;

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

/// An outer struct that holds DSTs in Arc and Rc, using `deserialize_with`.
#[derive(Serialize, serde::Deserialize)]
struct Dashboard {
    #[serde(deserialize_with = "LogEntry::deserialize_arc")]
    latest_log: Arc<LogEntry>,

    #[serde(deserialize_with = "SensorReading::deserialize_rc")]
    current_reading: Rc<SensorReading>,
}

fn main() {
    // --- str tail: Box round-trip ---

    let entry: Box<LogEntry> = LogEntry::build(3, "disk usage at 90%");
    println!("Original:     severity={}, message={:?}", entry.severity, &entry.message);

    let json = serde_json::to_string(&*entry).unwrap();
    println!("Serialized:   {json}");

    let restored: Box<LogEntry> = serde_json::from_str(&json).unwrap();
    println!("Deserialized: severity={}, message={:?}", restored.severity, &restored.message);

    assert_eq!(entry.severity, restored.severity);
    assert_eq!(&entry.message, &restored.message);

    // --- slice tail: Box round-trip ---

    let reading: Box<SensorReading> = SensorReading::build_from_slice(1_700_000_000, &[23.1, 23.4, 22.9, 23.0]);
    println!("\nOriginal:     timestamp={}, samples={:?}", reading.timestamp, &reading.samples);

    let json = serde_json::to_string(&*reading).unwrap();
    println!("Serialized:   {json}");

    let restored: Box<SensorReading> = serde_json::from_str(&json).unwrap();
    println!("Deserialized: timestamp={}, samples={:?}", restored.timestamp, &restored.samples);

    assert_eq!(reading.timestamp, restored.timestamp);
    assert_eq!(&reading.samples, &restored.samples);

    // --- Arc / Rc via deserialize_with ---

    let dashboard = Dashboard {
        latest_log: LogEntry::build_arc(2, "all systems normal"),
        current_reading: SensorReading::build_rc_from_slice(1_700_000_001, &[21.0, 21.5]),
    };

    let json = serde_json::to_string(&dashboard).unwrap();
    println!("\nDashboard serialized: {json}");

    let restored: Dashboard = serde_json::from_str(&json).unwrap();
    println!(
        "Dashboard deserialized: log=[{}, {:?}], reading=[{}, {:?}]",
        restored.latest_log.severity, &restored.latest_log.message, restored.current_reading.timestamp, &restored.current_reading.samples,
    );

    assert_eq!(&restored.latest_log.message, "all systems normal");
    assert_eq!(&restored.current_reading.samples, &[21.0, 21.5]);

    // Arc clone still works after deserialization
    let shared = Arc::clone(&restored.latest_log);
    println!("Arc strong count after clone: {}", Arc::strong_count(&shared));
}
