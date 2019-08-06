use cranelift::codegen::ir::types;
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

fn main() {
    // Set up a JIT builder.
    let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());

    // Set up a module using the SimpleJIT backend which outputs compiled functions to memory.
    let mut module: Module<SimpleJITBackend> = Module::new(builder);

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

    // Get the raw function pointer from the JIT.
    let function = module.get_finalized_function(functionid);
    // Cast the raw pointer a function pointer.
    let f: extern "C" fn(i32, i32) -> i32 = unsafe { std::mem::transmute(function) };

    // Print the result of the function.
    println!("14 + 48 = {}", f(14, 48));

    // Consume the module and turn it into the backend's product type, in this case, nothing.
    let _ = module.finish();
}
