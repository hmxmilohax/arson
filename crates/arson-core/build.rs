// SPDX-License-Identifier: LGPL-3.0-or-later

fn main() {
    let cfg = autocfg::new();

    autocfg::emit_possibility("error_generic_member_access");
    if cfg.probe_raw("#![feature(error_generic_member_access)]").is_ok() {
        autocfg::emit("error_generic_member_access");
    }
}
