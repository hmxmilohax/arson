// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

mod new;
mod noop;
mod old;

pub use new::*;
pub use noop::*;
pub use old::*;

pub trait CryptAlgorithm {
    fn next(&mut self) -> u8;
}

pub struct CryptReader<'read, 'crypt> {
    reader: &'read mut dyn io::Read,
    crypt: &'crypt mut dyn CryptAlgorithm,
}

impl<'read, 'crypt> CryptReader<'read, 'crypt> {
    pub fn new(reader: &'read mut impl io::Read, crypt: &'crypt mut impl CryptAlgorithm) -> Self {
        Self { reader, crypt }
    }
}

impl io::Read for CryptReader<'_, '_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let count = self.reader.read(buf)?;
        for byte in &mut buf[..count] {
            *byte ^= self.crypt.next();
        }

        Ok(count)
    }
}

pub struct CryptWriter<'write, 'crypt> {
    writer: &'write mut dyn io::Write,
    crypt: &'crypt mut dyn CryptAlgorithm,
}

impl<'write, 'crypt> CryptWriter<'write, 'crypt> {
    pub fn new(writer: &'write mut impl io::Write, crypt: &'crypt mut impl CryptAlgorithm) -> Self {
        Self { writer, crypt }
    }
}

impl io::Write for CryptWriter<'_, '_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut crypt_buf = [0u8; 512];

        let mut total_written = 0;
        while total_written < buf.len() {
            let end = (total_written + crypt_buf.len()).min(buf.len());
            let chunk = &buf[total_written..end];

            for i in 0..chunk.len() {
                crypt_buf[i] = chunk[i] ^ self.crypt.next();
            }

            let buf = &crypt_buf[..chunk.len()];
            let written = match self.writer.write(buf) {
                Ok(written) => written,
                Err(err) => {
                    // Write requires that Ok be returned if any bytes were consumed
                    if total_written > 0 {
                        return Ok(total_written);
                    } else {
                        return Err(err);
                    }
                },
            };

            total_written += written;
        }

        Ok(total_written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}
