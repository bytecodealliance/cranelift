use cranelift::codegen::ir::types;
use cranelift::prelude::*;
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieTrapCollection};
use cranelift_module::{Linkage, Module};

fn main() {
    // The Faerie backend only supports ISAs with PIC so we add a flag to say it's enabled.
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();

    // We use the native platform this program is running on.
    let isa_builder = cranelift_native::builder().unwrap();
    // We create the ISA we will target.
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));

    // Set up a builder using our target ISA.
    let builder = FaerieBuilder::new(
        isa,
        String::from("sum"),
        FaerieTrapCollection::Disabled,
        cranelift_module::default_libcall_names(),
    )
    .unwrap();

    // Set up a module using the FaerieBackend which outputs object files for the selected ISA.
    let mut module: Module<FaerieBackend> = Module::new(builder);

    // The context holds the state of the current code generation.
    let mut context = module.make_context();

    // Creating a function signature of `fn(i32, i32) -> i32`.
    context
        .func
        .signature
        .params
        .push(AbiParam::new(types::I32));
    context
        .func
        .signature
        .params
        .push(AbiParam::new(types::I32));
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::I32));

    // Debug name for the function, not required, usually ExternalName::User is used instead.
    context.func.name = cranelift::codegen::ir::ExternalName::TestCase {
        length: 3,
        ascii: [
            b's', b'u', b'm', b'\0', b'\0', b'\0', b'\0', b'\0', b'\0', b'\0', b'\0', b'\0', b'\0',
            b'\0', b'\0', b'\0',
        ],
    };

    // Function builder context holds the state for function builders.
    let mut fnbuilderctx = FunctionBuilderContext::new();
    // Create a new function builder to start building a function from the function signature in the context.
    let mut fnbuilder = FunctionBuilder::new(&mut context.func, &mut fnbuilderctx);

    // Create a new ebb block to be the body of the function.
    let entry_ebb = fnbuilder.create_ebb();
    // Copy the function parameters to the ebb parameters.
    fnbuilder.append_ebb_params_for_function_params(entry_ebb);

    // Point the builder to the ebb block.
    fnbuilder.switch_to_block(entry_ebb);
    // Seal the current block since there are no other blocks to make.
    fnbuilder.seal_block(entry_ebb);

    // Creating the variables for the function.
    let a = Variable::new(0);
    let b = Variable::new(1);

    // Declare the variable types using the function builder.
    fnbuilder.declare_var(a, types::I32);
    fnbuilder.declare_var(b, types::I32);

    // Set a to the first ebb param `fn(i32, i32) -> i32`.
    //                                  ^
    let atmp = fnbuilder.ebb_params(entry_ebb)[0];
    fnbuilder.def_var(a, atmp);

    // Set b to the second ebb param `fn(i32, i32) -> i32`.
    //                                        ^
    let btmp = fnbuilder.ebb_params(entry_ebb)[1];
    fnbuilder.def_var(b, btmp);

    // Create an add instruction to add a and b.
    // a + b
    let arg1 = fnbuilder.use_var(a);
    let arg2 = fnbuilder.use_var(b);
    let addins = fnbuilder.ins().iadd(arg1, arg2);

    // Create a return instruction to return the result of the add instruction.
    fnbuilder.ins().return_(&[addins]);

    // The function is done so we finallize the function builder.
    fnbuilder.finalize();

    // Declare the function in the module, must be done before they can be used in the module's configured backend.
    let functionid = module
        .declare_function("sum", Linkage::Export, &context.func.signature)
        .unwrap();
    // Defined the declared function which finishes the function compilation.
    module.define_function(functionid, &mut context).unwrap();

    // Print the Cranelift IR of the function to stdout.
    println!("{}", context.func.display(None));

    // Clear the context for reuse.
    module.clear_context(&mut context);

    // Finishes all functions in the module which finsihes all the patches and other realloctaions.
    module.finalize_definitions();

    // Consume the module and turn it into the backend's product type, in this case, a binary artifact.
    let result = module.finish();

    // Create a file to write to.
    let file = std::fs::File::create("sum.o").unwrap();
    // Write the binary artifact to an object file.
    result.artifact.write(file).unwrap();

    // Print disassembly of the compiled function on the terminal.
    std::process::Command::new("objdump")
        .arg("-d")
        .arg("sum.o")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    // Example C program using the function we just created.
    let csource = "#include <stdio.h>\n\
                   extern int sum(int, int);\n\
                   int main() {\n\
                   printf(\"14 + 48 = %d\\n\", sum(14, 48));\n\
                   }";

    // Writes the source to a temporary file.
    std::fs::write("tmp.c", csource).unwrap();

    // Compiles the C code linking it to our object file.
    std::process::Command::new("cc")
        .arg("tmp.c")
        .arg("sum.o")
        .arg("-o")
        .arg("tmp")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    // Executes temporary program.
    std::process::Command::new("./tmp")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    // Removes the temporary program, the object file, and the temporary C source code.
    std::process::Command::new("rm")
        .arg("sum.o")
        .arg("tmp.c")
        .arg("tmp")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}
