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

impl<Crypt: CryptAlgorithm> CryptAlgorithm for &mut Crypt {
    fn next(&mut self) -> u8 {
        (*self).next()
    }
}

pub struct CryptReader<Reader: io::Read, Crypt: CryptAlgorithm> {
    reader: Reader,
    crypt: Crypt,
}

impl<R: io::Read, C: CryptAlgorithm> CryptReader<R, C> {
    pub fn new(reader: R, crypt: C) -> Self {
        Self { reader, crypt }
    }

    pub fn into_inner(self) -> R {
        self.reader
    }
}

impl<R: io::Read, C: CryptAlgorithm> io::Read for CryptReader<R, C> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let count = self.reader.read(buf)?;
        for byte in &mut buf[..count] {
            *byte ^= self.crypt.next();
        }

        Ok(count)
    }
}

pub struct CryptWriter<Writer: io::Write, Crypt: CryptAlgorithm> {
    writer: Writer,
    crypt: Crypt,
}

impl<W: io::Write, C: CryptAlgorithm> CryptWriter<W, C> {
    pub fn new(writer: W, crypt: C) -> Self {
        Self { writer, crypt }
    }
}

impl<W: io::Write, C: CryptAlgorithm> io::Write for CryptWriter<W, C> {
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
