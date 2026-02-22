use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};

use crate::error::{BelError, BelResult};
use crate::value::StreamId;

/// The inner state of a stream.
pub enum StreamInner {
    StdinStream {
        /// Bit buffer for reading: bits are extracted from bytes MSB first.
        byte_buf: Option<u8>,
        bit_pos: u8, // 0-7, next bit to read (MSB=0)
    },
    StdoutStream {
        /// Bit buffer for writing: bits are accumulated into bytes MSB first.
        byte_buf: u8,
        bit_pos: u8, // 0-7, next bit position to write
    },
    FileIn {
        reader: BufReader<File>,
        byte_buf: Option<u8>,
        bit_pos: u8,
    },
    FileOut {
        writer: BufWriter<File>,
        byte_buf: u8,
        bit_pos: u8,
    },
    Closed,
}

/// Manages all open streams.
pub struct StreamTable {
    streams: Vec<StreamInner>,
}

impl StreamTable {
    pub fn new() -> Self {
        // StreamId(0) = stdin, StreamId(1) = stdout
        StreamTable {
            streams: vec![
                StreamInner::StdinStream {
                    byte_buf: None,
                    bit_pos: 0,
                },
                StreamInner::StdoutStream {
                    byte_buf: 0,
                    bit_pos: 0,
                },
            ],
        }
    }

    /// Open a new stream for reading or writing.
    /// `name` is the file path, `direction` is "in" or "out".
    pub fn open(&mut self, name: &str, direction: &str) -> BelResult<StreamId> {
        let inner = match direction {
            "in" => {
                let file = File::open(name)
                    .map_err(|e| BelError::IoError(format!("cannot open '{}': {}", name, e)))?;
                StreamInner::FileIn {
                    reader: BufReader::new(file),
                    byte_buf: None,
                    bit_pos: 0,
                }
            }
            "out" => {
                let file = File::create(name)
                    .map_err(|e| BelError::IoError(format!("cannot create '{}': {}", name, e)))?;
                StreamInner::FileOut {
                    writer: BufWriter::new(file),
                    byte_buf: 0,
                    bit_pos: 0,
                }
            }
            _ => return Err(BelError::IoError(format!("invalid direction: {}", direction))),
        };

        let id = StreamId(self.streams.len() as u32);
        self.streams.push(inner);
        Ok(id)
    }

    /// Close a stream, flushing any pending bits.
    pub fn close(&mut self, id: StreamId) -> BelResult<()> {
        let stream = &mut self.streams[id.0 as usize];
        // Flush pending write bits before closing
        match stream {
            StreamInner::StdoutStream { byte_buf, bit_pos } if *bit_pos > 0 => {
                let byte = *byte_buf;
                io::stdout().write_all(&[byte]).map_err(|e| BelError::IoError(e.to_string()))?;
                io::stdout().flush().map_err(|e| BelError::IoError(e.to_string()))?;
            }
            StreamInner::FileOut { writer, byte_buf, bit_pos } if *bit_pos > 0 => {
                let byte = *byte_buf;
                writer.write_all(&[byte]).map_err(|e| BelError::IoError(e.to_string()))?;
                writer.flush().map_err(|e| BelError::IoError(e.to_string()))?;
            }
            _ => {}
        }
        self.streams[id.0 as usize] = StreamInner::Closed;
        Ok(())
    }

    /// Get the status of a stream: "closed", "in", or "out".
    pub fn status(&self, id: StreamId) -> &'static str {
        match &self.streams[id.0 as usize] {
            StreamInner::Closed => "closed",
            StreamInner::StdinStream { .. } | StreamInner::FileIn { .. } => "in",
            StreamInner::StdoutStream { .. } | StreamInner::FileOut { .. } => "out",
        }
    }

    /// Write a single bit (true=1, false=0) to a stream.
    pub fn write_bit(&mut self, id: StreamId, bit: bool) -> BelResult<()> {
        let stream = &mut self.streams[id.0 as usize];
        match stream {
            StreamInner::StdoutStream { byte_buf, bit_pos } => {
                if bit {
                    *byte_buf |= 1 << (7 - *bit_pos);
                }
                *bit_pos += 1;
                if *bit_pos == 8 {
                    let byte = *byte_buf;
                    io::stdout().write_all(&[byte]).map_err(|e| BelError::IoError(e.to_string()))?;
                    io::stdout().flush().map_err(|e| BelError::IoError(e.to_string()))?;
                    *byte_buf = 0;
                    *bit_pos = 0;
                }
                Ok(())
            }
            StreamInner::FileOut { writer, byte_buf, bit_pos } => {
                if bit {
                    *byte_buf |= 1 << (7 - *bit_pos);
                }
                *bit_pos += 1;
                if *bit_pos == 8 {
                    let byte = *byte_buf;
                    writer.write_all(&[byte]).map_err(|e| BelError::IoError(e.to_string()))?;
                    *byte_buf = 0;
                    *bit_pos = 0;
                }
                Ok(())
            }
            StreamInner::Closed => Err(BelError::IoError("write to closed stream".into())),
            _ => Err(BelError::IoError("write to input stream".into())),
        }
    }

    /// Read a single bit from a stream.
    /// Returns Some(true) for 1, Some(false) for 0, None if no bit available yet.
    /// Returns Err with a special "eof" marker if the stream is exhausted.
    pub fn read_bit(&mut self, id: StreamId) -> BelResult<Option<bool>> {
        let stream = &mut self.streams[id.0 as usize];
        match stream {
            StreamInner::StdinStream { byte_buf, bit_pos } => {
                if byte_buf.is_none() {
                    let mut buf = [0u8; 1];
                    match io::stdin().read(&mut buf) {
                        Ok(0) => return Err(BelError::IoError("eof".into())),
                        Ok(_) => {
                            *byte_buf = Some(buf[0]);
                            *bit_pos = 0;
                        }
                        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                            return Ok(None);
                        }
                        Err(e) => return Err(BelError::IoError(e.to_string())),
                    }
                }
                if let Some(byte) = byte_buf {
                    let bit = (*byte >> (7 - *bit_pos)) & 1 == 1;
                    *bit_pos += 1;
                    if *bit_pos == 8 {
                        *byte_buf = None;
                        *bit_pos = 0;
                    }
                    Ok(Some(bit))
                } else {
                    Ok(None)
                }
            }
            StreamInner::FileIn { reader, byte_buf, bit_pos } => {
                if byte_buf.is_none() {
                    let mut buf = [0u8; 1];
                    match reader.read(&mut buf) {
                        Ok(0) => return Err(BelError::IoError("eof".into())),
                        Ok(_) => {
                            *byte_buf = Some(buf[0]);
                            *bit_pos = 0;
                        }
                        Err(e) => return Err(BelError::IoError(e.to_string())),
                    }
                }
                if let Some(byte) = byte_buf {
                    let bit = (*byte >> (7 - *bit_pos)) & 1 == 1;
                    *bit_pos += 1;
                    if *bit_pos == 8 {
                        *byte_buf = None;
                        *bit_pos = 0;
                    }
                    Ok(Some(bit))
                } else {
                    Ok(None)
                }
            }
            StreamInner::Closed => Err(BelError::IoError("read from closed stream".into())),
            _ => Err(BelError::IoError("read from output stream".into())),
        }
    }

    /// Get the stdin stream ID (for nil input stream).
    pub fn stdin_id() -> StreamId {
        StreamId(0)
    }

    /// Get the stdout stream ID (for nil output stream).
    pub fn stdout_id() -> StreamId {
        StreamId(1)
    }
}
