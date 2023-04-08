use front::lower;

pub fn choco_val_to_str(param: i64) -> String {
    if param == lower::TRUE {
        "True".to_owned()
    } else if param == lower::FALSE {
        "False".to_owned()
    } else if param == lower::NONE {
        "None".to_owned()
    } else if (param & 0x03) == lower::OBJ_TAG {
        "obj".to_owned()
    } else {
        format!("{}", ((param & 0x0000_0003_ffff_ffff) >> 2) as i32)
    }
}
