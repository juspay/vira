import { test } from "@playwright/test";
import { MainPage } from "../pages/main-page";

test.describe("Main Page", () => {
  test("should load successfully and display navigation", async ({ page }) => {
    const mainPage = new MainPage(page);

    await mainPage.goto();
    await mainPage.verifyPageLoaded();
    await mainPage.verifyNavigationLinks();
  });
});
