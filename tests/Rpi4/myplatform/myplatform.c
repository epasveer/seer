#include <linux/module.h>
#include <linux/platform_device.h>
#include <linux/of.h>
#include <linux/sysfs.h>
#include <linux/slab.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Developer");
MODULE_DESCRIPTION("Simple platform driver with DT compatible and sysfs for Raspberry Pi 4");
MODULE_VERSION("1.0");

struct myplatform_dev {
    struct device   *dev;
    u32              id;
};

/* sysfs: read the device id assigned at probe */
static ssize_t id_show(struct device *dev, struct device_attribute *attr, char *buf)
{
    struct myplatform_dev *priv = dev_get_drvdata(dev);
    return sysfs_emit(buf, "%u\n", priv->id);
}
static DEVICE_ATTR_RO(id);

static struct attribute *myplatform_attrs[] = {
    &dev_attr_id.attr,
    NULL,
};
ATTRIBUTE_GROUPS(myplatform);

static int myplatform_probe(struct platform_device *pdev)
{
    struct myplatform_dev *priv;
    u32 id = 0;

    priv = devm_kzalloc(&pdev->dev, sizeof(*priv), GFP_KERNEL);
    if (!priv)
        return -ENOMEM;

    priv->dev = &pdev->dev;

    /* Read optional "id" property from DT node, default 0 */
    of_property_read_u32(pdev->dev.of_node, "id", &id);
    priv->id = id;

    platform_set_drvdata(pdev, priv);

    dev_info(&pdev->dev, "myplatform: probed (id=%u, node=%s)\n",
             priv->id, pdev->dev.of_node->name);
    return 0;
}

static int myplatform_remove(struct platform_device *pdev)
{
    dev_info(&pdev->dev, "myplatform: removed\n");
    return 0;
}

static const struct of_device_id myplatform_of_match[] = {
    { .compatible = "rpi4,myplatform" },
    { /* sentinel */ }
};
MODULE_DEVICE_TABLE(of, myplatform_of_match);

static struct platform_driver myplatform_driver = {
    .probe  = myplatform_probe,
    .remove = myplatform_remove,
    .driver = {
        .name           = "myplatform",
        .of_match_table = myplatform_of_match,
        .dev_groups     = myplatform_groups,
    },
};

/* Expands to module_init / module_exit wrappers around
 * platform_driver_register / platform_driver_unregister */
module_platform_driver(myplatform_driver);
