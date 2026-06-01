#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sysfs.h>
#include <linux/kobject.h>
#include <linux/slab.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Developer");
MODULE_DESCRIPTION("Advanced Hello World kernel module with sysfs and Fibonacci calculation for Raspberry Pi 4");
MODULE_VERSION("1.0");

// Global variables for sysfs
static struct kobject *helloworld_kobj;
static unsigned long fib_result = 0;
static unsigned int fib_n = 0;

// Calculate Fibonacci number iteratively (n >= 0)
static unsigned long calculate_fibonacci(unsigned int n) {
    unsigned long a = 0, b = 1, c;
    int i;

    if (n <= 0)
        return 0;
    if (n == 1)
        return 1;

    for (i = 2; i <= n; i++) {
        c = a + b;
        a = b;
        b = c;
    }
    return b;
}

// Sysfs show function (read /sys/kernel/helloworld/fibonacci)
static ssize_t fibonacci_show(struct kobject *kobj, struct kobj_attribute *attr, char *buf) {
    return sprintf(buf, "Fibonacci(%u) = %lu\n", fib_n, fib_result);
}

// Sysfs store function (write to /sys/kernel/helloworld/fibonacci)
static ssize_t fibonacci_store(struct kobject *kobj, struct kobj_attribute *attr, const char *buf, size_t count) {
    int ret;

    // Parse input as unsigned int
    ret = kstrtouint(buf, 10, &fib_n);
    if (ret < 0) {
        printk(KERN_ERR "helloworld: Invalid input for n\n");
        return ret;
    }

    // Limit n to avoid overflow (Fibonacci grows quickly)
    if (fib_n > 93) {  // unsigned long can safely handle up to Fib(93)
        printk(KERN_ERR "helloworld: n=%u is too large (max 93)\n", fib_n);
        return -EINVAL;
    }

    // Calculate Fibonacci
    fib_result = calculate_fibonacci(fib_n);
    printk(KERN_INFO "helloworld: Calculated Fibonacci(%u) = %lu\n", fib_n, fib_result);

    return count;
}

// Define sysfs attribute
static struct kobj_attribute fibonacci_attr = __ATTR(fibonacci, 0664, fibonacci_show, fibonacci_store);

// Module initialization
static int __init helloworld_init(void) {
    int ret;

    printk(KERN_INFO "Hello, World! Initializing advanced module on Raspberry Pi 4\n");

    // Create sysfs directory under /sys/kernel/helloworld
    helloworld_kobj = kobject_create_and_add("helloworld", kernel_kobj);
    if (!helloworld_kobj) {
        printk(KERN_ERR "helloworld: Failed to create kobject\n");
        return -ENOMEM;
    }

    // Create sysfs file /sys/kernel/helloworld/fibonacci
    ret = sysfs_create_file(helloworld_kobj, &fibonacci_attr.attr);
    if (ret) {
        printk(KERN_ERR "helloworld: Failed to create sysfs file\n");
        kobject_put(helloworld_kobj);
        return ret;
    }

    printk(KERN_INFO "helloworld: Sysfs interface created at /sys/kernel/helloworld/fibonacci\n");
    return 0;
}

// Module cleanup
static void __exit helloworld_exit(void) {
    printk(KERN_INFO "Goodbye, World! Cleaning up module on Raspberry Pi 4\n");
    if (helloworld_kobj) {
        sysfs_remove_file(helloworld_kobj, &fibonacci_attr.attr);
        kobject_put(helloworld_kobj);
    }
}

module_init(helloworld_init);
module_exit(helloworld_exit);
