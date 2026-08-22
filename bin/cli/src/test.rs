use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use vfs::Vfs;

use crate::{analyzing, cli::OptLevel, compiling};

struct TempProject(PathBuf);

impl Drop for TempProject {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn test_multi_module_object_generation() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time should be after Unix epoch")
        .as_nanos();
    let project_dir =
        std::env::temp_dir().join(format!("airyc-module-test-{}-{unique}", std::process::id()));
    std::fs::create_dir_all(&project_dir).expect("failed to create temporary project directory");
    let _cleanup = TempProject(project_dir.clone());

    let dependency_path = project_dir.join("dependency.airy");
    let entry_path = project_dir.join("entry.airy");

    std::fs::write(
        &dependency_path,
        r#"
        struct Item {
            value: i32,
        }

        fn read_item(item: *const struct Item) -> i32 {
            return item->value;
        }
        "#,
    )
    .expect("failed to write dependency module");
    std::fs::write(
        &entry_path,
        r#"
        import "dependency.airy" :: Item
        import "dependency.airy" :: read_item

        fn main() -> i32 {
            let item: struct Item = {41};
            return read_item(&item) + later();
        }

        fn later() -> i32 {
            return 1;
        }
        "#,
    )
    .expect("failed to write entry module");

    let vfs = Vfs::default();
    let project = analyzing::analyze_project(&[entry_path, dependency_path], &vfs)
        .expect("multi-module analysis should succeed");
    let objects = compiling::compile_project_to_object_bytes(&project, &vfs, OptLevel::None)
        .expect("multi-module object generation should succeed");

    assert_eq!(objects.len(), 2);
    assert!(objects.iter().all(|(_, bytes)| !bytes.is_empty()));
}
