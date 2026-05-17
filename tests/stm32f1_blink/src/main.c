#include <stm32f1xx_hal.h>
#include "advanced.h"
#define LED_PIN GPIO_PIN_13
#define LED_GPIO_PORT GPIOC
#define LED_GPIO_CLK_ENABLE() __HAL_RCC_GPIOC_CLK_ENABLE()
int i1 = 0;
int i2 = 0;
int i3 = 0;

familyStruct globalStruct = {
    // DAD
    {
        50,
        "DAD_NAME",
        {
            1,
            "1st_child"
        }
    },
    4, "FAMILY",
    // MOM
    {
        45,
        "MOM_NAME",
        {
            {'a',
            "a_child"},
        }

    }
};

void init_clock() {
    RCC_OscInitTypeDef RCC_OscInitStruct = {0};
    RCC_ClkInitTypeDef RCC_ClkInitStruct = {0};

    /** Initializes the RCC Oscillators according to the specified parameters
     * in the RCC_OscInitTypeDef structure.
     */
    RCC_OscInitStruct.OscillatorType = RCC_OSCILLATORTYPE_HSI;
    RCC_OscInitStruct.HSIState = RCC_HSI_ON;
    RCC_OscInitStruct.HSICalibrationValue = RCC_HSICALIBRATION_DEFAULT;
    RCC_OscInitStruct.PLL.PLLState = RCC_PLL_NONE;
    HAL_RCC_OscConfig(&RCC_OscInitStruct);

    // Initializes the CPU, AHB and APB buses clocks
    RCC_ClkInitStruct.ClockType = RCC_CLOCKTYPE_HCLK | RCC_CLOCKTYPE_SYSCLK |
                                  RCC_CLOCKTYPE_PCLK1 | RCC_CLOCKTYPE_PCLK2;
    RCC_ClkInitStruct.SYSCLKSource = RCC_SYSCLKSOURCE_HSI;
    RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
    RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV1;
    HAL_RCC_ClockConfig(&RCC_ClkInitStruct, FLASH_LATENCY_0);

    LED_GPIO_CLK_ENABLE();
}

void init_gpio() {
    GPIO_InitTypeDef GPIO_InitStruct;

    GPIO_InitStruct.Pin = LED_PIN;
    GPIO_InitStruct.Mode = GPIO_MODE_OUTPUT_PP;
    GPIO_InitStruct.Pull = GPIO_PULLUP;
    GPIO_InitStruct.Speed = GPIO_SPEED_FREQ_HIGH;
    HAL_GPIO_Init(LED_GPIO_PORT, &GPIO_InitStruct);
}

void init() {
    HAL_Init();
    init_clock();
    init_gpio();
}

int main(void) {
    init();

    while (1) {
        globalStruct.numberOfMember++;
        globalStruct.dad.age ++;
        globalStruct.familyName[1] ++;
        globalStruct.dad.name[0] ++;
        globalStruct.mom.child.name[0] ++;
        globalStruct.dad.child.name[1] ++;
        i1 ++;
        i2 += 2;
        i3 += 3;
        HAL_GPIO_TogglePin(LED_GPIO_PORT, LED_PIN);
        HAL_Delay(500);
    }
}

void SysTick_Handler() { HAL_IncTick(); }
